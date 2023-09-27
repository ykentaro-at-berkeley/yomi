# README.md

## Overview

The `edsl.ml` module is an OCaml implementation for a card game, providing a robust framework for handling game logic, moves, and scoring. It defines various types and functions to represent and manipulate the game state, cards, players, and moves. The module is designed to be extensible and customizable for different card game variations.

## Key Features

- **Card Representation**: Cards are represented with months and ranks.
- **Game Phases**: The game can be in different phases such as `Play_phase`, `Awase1_phase`, `Awase2_phase`, `Winning_phase`, and `Thru_phase`.
- **Move Application**: Apply different types of moves to the game state and transition between different game phases.
- **Scoring**: Calculate the score based on the cards collected by players.
- **Yaku Handling**: Handle special card combinations (yaku) and calculate additional points.
- **Monte Carlo Tree Search**: Implement Monte Carlo Tree Search for AI players to make optimal moves.

## How to Use

### Game Initialization

To initialize a game, use the `init` function:

```ocaml
let game = TestGame.init ()
```

This function will return an initial game state with shuffled cards.

### Applying Moves
To apply a move to the game state, use the apply function:

```ocaml
let TestGame.GExist new_game = TestGame.apply game move
```

Where game is the current game state and move is the move you want to apply. The apply function will return a new game state after applying the move.

### Calculating Score
To calculate the score of a game state, use the payoff function:

```ocaml
let score = TestGame.payoff game
```

Where game is the game state you want to calculate the score for. The payoff function will return the scores for both players.

### Handling Yaku
To handle yaku in the game, use the yaku_results_of_tori function:

```ocaml
let results = TestGame.yaku_results_of_tori tori
```

Where tori is the collected cards of a player. The yaku_results_of_tori function will return the yaku results for the given cards.

### Running AI Player
To make an AI player make a move, use the good_move function from the MCUCB1 module:

```ocaml
let move = TestGame.MCUCB1.good_move game
```

Where game is the current game state. The good_move function will return the best move for the AI player based on Monte Carlo Tree Search.

### Testing
To ensure the correctness of the module, comprehensive tests are provided. Run the tests to verify the functionality of the game logic, move application, scoring, and other aspects of the module.

