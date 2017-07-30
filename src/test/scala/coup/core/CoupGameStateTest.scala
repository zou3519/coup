package coup.core

import coup.core.Character._
import org.scalatest.{GivenWhenThen, Matchers}

class CoupGameStateTest extends org.scalatest.FeatureSpec
  with GivenWhenThen
  with Matchers {

  // TODO: with so much going on, assertEquality doesn't really make sense
  def assertEquality(
    testGameState: CoupGameState,
    correctGameState: CoupGameState,
    testCourtDeck: Boolean = false,
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

  def assertPlayerLostInfluence(
      player: PlayerT,
      influence: Character.EnumVal,
      oldGameState: CoupGameState,
      newGameState: CoupGameState): Unit = {
    newGameState.influences(player) :+ influence should contain theSameElementsAs
      oldGameState.influences(player)
    newGameState.discardPile(player) should contain theSameElementsAs
      oldGameState.discardPile(player) :+ influence
  }

  def assertPlayerInfluencesAreUnchanged(
      player: PlayerT,
      oldGameState: CoupGameState,
      newGameState: CoupGameState): Unit = {
    newGameState.influences(player) should contain theSameElementsAs
      oldGameState.influences(player)
    newGameState.discardPile(player) should contain theSameElementsAs
      oldGameState.discardPile(player)
  }

  def nextPlayer(player: PlayerT): PlayerT = (player + 1)%2

  feature("income") {
    scenario("first player incomes") {
      Given("a game state")
      val player = 0
      val newGameState = CoupGameState.init()
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
      val newGameState = CoupGameState.init()
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
      val newGameState = CoupGameState.init(coins = Vector(7, 7))
      val oldGameState = newGameState.copy

      When("A coup happens [coup => lose influence]")
      newGameState.applyAction(Coup(player, otherPlayer))
      val lostInfluence = newGameState.influences(otherPlayer)(0)
      newGameState.applyAction(LoseInfluence(otherPlayer, lostInfluence))

      Then("the player pays 7 coins")
      assert((newGameState.coins(player) + 7) == oldGameState.coins(player))

      And("the other player loses a influence")
      assertPlayerLostInfluence(otherPlayer, lostInfluence, oldGameState, newGameState)
      assertPlayerInfluencesAreUnchanged(player, oldGameState, newGameState)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)
    }

    scenario("must coup") {
      Given("a game state where players have 10 coins each")
      val player = 0
      val newGameState = CoupGameState.init(coins = Vector(10, 10))

      When("the player moves")

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
      val newGameState = CoupGameState.init()
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
      val newGameState = CoupGameState.init()
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

    scenario("foreign aid => block => challenge => prove influence => discard influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Contessa, Contessa),
        Vector(Duke, Assassin))
      val oldGameState = newGameState.copy

      When("foreign aid => block => challenge => prove influence => discard influence")
      newGameState.applyAction(ForeignAid(player))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(Challenge(player, otherPlayer))
      newGameState.applyAction(ProveInfluence(otherPlayer, Duke))
      val lostInfluence = Contessa
      newGameState.applyAction(LoseInfluence(player, lostInfluence))

      Then("coins is unchanged")
      assert(newGameState.coins == oldGameState.coins)

      And("only the challenging player loses an influence")
      assertPlayerLostInfluence(player, lostInfluence, oldGameState, newGameState)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)
    }

    scenario("foreign aid => block => challenge => lose influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Contessa, Contessa),
        Vector(Ambassador, Assassin))
      val oldGameState = newGameState.copy

      When("foreign aid => block => challenge => lose influence")
      newGameState.applyAction(ForeignAid(player))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(Challenge(player, otherPlayer))
      val lostInfluence = Assassin
      newGameState.applyAction(LoseInfluence(otherPlayer, lostInfluence))

      Then("only the player gains 2 coins")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 2)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer))

      And("only the challenging player loses an influence")
      assertPlayerLostInfluence(otherPlayer, lostInfluence, oldGameState, newGameState)
      assertPlayerInfluencesAreUnchanged(player, oldGameState, newGameState)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)
    }
  }

  feature("steal") {
    scenario("steal => do nothing") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init(coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => do nothing")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(DoNothing(otherPlayer))

      Then("the player stole 2 coins")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 2)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer) - 2)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testInfluences = false,
        testDiscardPile = false,
        testCoins = false)
    }

    scenario("steal when other player has 1 coin") {
      Given("a game state where other player has 1 coin")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init(coins = Vector(2, 1))
      val oldGameState = newGameState.copy


      When("steal => do nothing")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(DoNothing(otherPlayer))

      Then("the player stole 1 coin")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 1)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer) - 1)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false,
        testCoins = false)
    }


    scenario("steal => challenge => lose influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init(coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => challenge => lose influence")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Challenge(otherPlayer, player))
      val lostInfluence = newGameState.influences(player)(1)
      newGameState.applyAction(LoseInfluence(player, lostInfluence))

      Then("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("only the challenged player loses an influence")
      assertPlayerLostInfluence(player, lostInfluence, oldGameState, newGameState)
      assertPlayerInfluencesAreUnchanged(otherPlayer, oldGameState, newGameState)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false)
    }

    scenario("steal => challenge => prove influence => lose influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Captain, Ambassador),
        Vector(Assassin, Duke),
        coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => challenge => prove influence => lose influence")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Challenge(otherPlayer, player))
      newGameState.applyAction(ProveInfluence(player, Captain))
      val lostInfluence = newGameState.influences(otherPlayer)(1)
      newGameState.applyAction(LoseInfluence(otherPlayer, lostInfluence))

      Then("the player stole two coins")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 2)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer) - 2)

      And("only the challenging player loses an influence")
      assertPlayerLostInfluence(otherPlayer, lostInfluence, oldGameState, newGameState)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false,
        testCoins = false)
    }

    scenario("steal => block => do nothing") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.init(coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => block => do nothing")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(DoNothing(player))

      Then("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false)
    }

    scenario("steal => block => challenge " +
        "prove influence (captain) => lose influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Duke, Assassin),
        Vector(Captain, Captain),
        coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => block => challenge " +
        "prove influence (captain) => lose influence")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(Challenge(player, otherPlayer))
      newGameState.applyAction(ProveInfluence(otherPlayer, Captain))
      val lostInfluence = Duke
      newGameState.applyAction(LoseInfluence(player, lostInfluence))

      Then("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("only the challenging player loses an influence")
      assertPlayerLostInfluence(player, lostInfluence, oldGameState, newGameState)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false)
    }

    scenario("steal => block => challenge " +
        "prove influence (ambassador) => lose influence") {
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Duke, Assassin),
        Vector(Ambassador, Duke),
        coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => block => challenge " +
        "prove influence (captain) => lose influence")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(Challenge(player, otherPlayer))
      newGameState.applyAction(ProveInfluence(otherPlayer, Ambassador))
      val lostInfluence = Duke
      newGameState.applyAction(LoseInfluence(player, lostInfluence))

      Then("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("only the challenging player loses an influence")
      assertPlayerLostInfluence(player, lostInfluence, oldGameState, newGameState)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false)
    }

    scenario("steal => block => challenge => lose influence") {
      Given("a game state")
      val player = 0
      val otherPlayer = nextPlayer(player)
      val newGameState = CoupGameState.initWith(
        Vector(Captain, Ambassador),
        Vector(Assassin, Duke),
        coins = Vector(2, 2))
      val oldGameState = newGameState.copy

      When("steal => block => challenge => lose influence")
      newGameState.applyAction(Steal(player, otherPlayer))
      newGameState.applyAction(Block(otherPlayer, player))
      newGameState.applyAction(Challenge(player, otherPlayer))
      val lostInfluence = newGameState.influences(otherPlayer)(1)
      newGameState.applyAction(LoseInfluence(otherPlayer, lostInfluence))

      Then("the player stole 2 coins")
      assert(newGameState.coins(player) == oldGameState.coins(player) + 2)
      assert(newGameState.coins(otherPlayer) == oldGameState.coins(otherPlayer) - 2)

      And("only the challenged player loses an influence")
      assertPlayerLostInfluence(otherPlayer, lostInfluence, oldGameState, newGameState)
      assertPlayerInfluencesAreUnchanged(player, oldGameState, newGameState)

      And("it is the next player's turn")
      assert(newGameState.pendingStages.head.player != player)
      assert(newGameState.currentPlay.isEmpty)

      And("everything else is unchanged")
      assertEquality(
        newGameState,
        oldGameState,
        testDiscardPile = false,
        testInfluences = false,
        testCoins = false)
    }
  }
}
