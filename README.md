## Blackjack

This is a Haskell implementation of blackjack.

- The game allows for any number of players (need to cap it)
- It allows each player to bet on four hands.
- It deals cards correctly to all player hands and the dealer
- It allows for an insurance bet up to half of all bet amounts
- Each hand allows for Hit/Stand/Double/Split when appropriate
- Currently only has dealer hitting on all 16s
- Processes blackjack on all two card hands (need to implement no BJ on splits)
- Deals cards with a cut card to reshuffle
- Can shuffle discards to finish out a hand if shoe runs out


To run, clone this repository. In the top level folder, run 
```
stack init
```
and you and then start the program with
```
stack build
stack run
```
Each player starts with 1000 (chips/dollars/Money)

Ctrl+C to quit out or if you all players lose all of their money the game ends.